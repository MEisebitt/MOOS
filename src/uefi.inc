struc int8
    .data: resb 1
endstruc

struc int16
    .data: resw 1
endstruc

struc int32
    .data: resd 1
endstruc

struc int64
    .data: resq 1
endstruc

struc dptr
    .data: resq 1
endstruc


EFI_SUCCESS			    equ 0x8000000000000000
EFI_LOAD_ERROR			equ 0x8000000000000001
EFI_INVALID_PARAMETER	equ 0x8000000000000002
EFI_UNSUPPORTED 		equ 0x8000000000000003
EFI_BAD_BUFFER_SIZE		equ 0x8000000000000004
EFI_BUFFER_TOO_SMALL	equ 0x8000000000000005
EFI_NOT_READY			equ 0x8000000000000006
EFI_DEVICE_ERROR		equ 0x8000000000000007
EFI_WRITE_PROTECTED		equ 0x8000000000000008
EFI_OUT_OF_RESOURCES	equ 0x8000000000000009
EFI_VOLUME_CORRUPTED	equ 0x800000000000000a
EFI_VOLUME_FULL 		equ 0x800000000000000b
EFI_NO_MEDIA			equ 0x800000000000000c
EFI_MEDIA_CHANGED		equ 0x800000000000000d
EFI_NOT_FOUND			equ 0x800000000000000e
EFI_ACCESS_DENIED		equ 0x800000000000000f
EFI_NO_RESPONSE 		equ 0x8000000000000010
EFI_NO_MAPPING			equ 0x8000000000000011
EFI_TIMEOUT			    equ 0x8000000000000012
EFI_NOT_STARTED 		equ 0x8000000000000013
EFI_ALREADY_STARTED		equ 0x8000000000000014
EFI_ABORTED			    equ 0x8000000000000015
EFI_ICMP_ERROR			equ 0x8000000000000016
EFI_TFTP_ERROR			equ 0x8000000000000017
EFI_PROTOCOL_ERROR		equ 0x8000000000000018


EFI_SYSTEM_TABLE_SIGNATURE equ 0x4942492053595354

struc EFI_TABLE_HEADER
    .Signature:     resq 1
    .Revision:      resd 1
    .HeaderSize:    resd 1
    .CRC32:         resd 1
    .Reserved:      resd 1
endstruc

struc EFI_SYSTEM_TABLE
    .Hdr		        EFI_TABLE_HEADER
    .FirmwareVendor:        resq 1
    .FirmwareRevision:      resd 1
    .ConsoleInHandle:       resq 1
    .ConIn:                 resq 1
    .ConsoleOutHandle:      resq 1
    .ConOut:                resq 1
    .StandardErrorHandle:   resq 1
    .StdErr:                resq 1
    .RuntimeServices:       resq 1
    .BootServices:          resq 1
    .NumberOfTableEntries:  resq 1
    .ConfigurationTable:    resq 1
endstruc
 
struc SIMPLE_TEXT_OUTPUT_INTERFACE {
 .Reset 	    	dptr
 .OutputString	    	dptr
 .TestString	    	dptr
 .QueryMode	    	dptr
 .SetMode	    	dptr
 .SetAttribute	    	dptr
 .ClearScreen	    	dptr
 .SetCursorPosition 	dptr
 .EnableCursor	    	dptr
 .Mode		    	dptr
}
struct SIMPLE_TEXT_OUTPUT_INTERFACE
 
;---include ends
 
struc SIMPLE_INPUT_INTERFACE {
 .Reset			dptr
 .ReadKeyStroke		dptr
 .WaitForKey		dptr
}
struct SIMPLE_INPUT_INTERFACE
 
struc EFI_BOOT_SERVICES_TABLE {
 .Hdr		       	EFI_TABLE_HEADER
 .RaisePriority		dptr
 .RestorePriority	dptr
 .AllocatePages		dptr
 .FreePages		dptr
 .GetMemoryMap		dptr
 .AllocatePool		dptr
 .FreePool		dptr
 .CreateEvent		dptr
 .SetTimer		dptr
 .WaitForEvent		dptr
 .SignalEvent		dptr
 .CloseEvent		dptr
 .CheckEvent		dptr
 .InstallProtocolInterface dptr
 .ReInstallProtocolInterface dptr
 .UnInstallProtocolInterface dptr
 .HandleProtocol	dptr
 .Void			dptr
 .RegisterProtocolNotify dptr
 .LocateHandle		dptr
 .LocateDevicePath	dptr
 .InstallConfigurationTable dptr
 .ImageLoad		dptr
 .ImageStart		dptr
 .Exit			dptr
 .ImageUnLoad		dptr
 .ExitBootServices	dptr
 .GetNextMonotonicCount	dptr
 .Stall			dptr
 .SetWatchdogTimer	dptr
 .ConnectController	dptr
 .DisConnectController	dptr
 .OpenProtocol		dptr
 .CloseProtocol		dptr
 .OpenProtocolInformation dptr
 .ProtocolsPerHandle	dptr
 .LocateHandleBuffer	dptr
 .LocateProtocol	dptr
 .InstallMultipleProtocolInterfaces dptr
 .UnInstallMultipleProtocolInterfaces dptr
 .CalculateCrc32	dptr
 .CopyMem		dptr
 .SetMem		dptr
}
struct EFI_BOOT_SERVICES_TABLE
 
struc EFI_RUNTIME_SERVICES_TABLE {
 .Hdr		       	EFI_TABLE_HEADER
 .GetTime		dptr
 .SetTime		dptr
 .GetWakeUpTime		dptr
 .SetWakeUpTime		dptr
 .SetVirtualAddressMap	dptr
 .ConvertPointer	dptr
 .GetVariable		dptr
 .GetNextVariableName	dptr
 .SetVariable		dptr
 .GetNextHighMonoCount	dptr
 .ResetSystem		dptr
}
struct EFI_RUNTIME_SERVICES_TABLE
 
struc EFI_TIME {
 .Year			int16
 .Month			int8
 .Day			int8
 .Hour			int8
 .Minute		int8
 .Second		int8
 .Pad1			int8
 .Nanosecond		int32
 .TimeZone		int16
 .Daylight		int8
 .Pad2			int8
 .sizeof		rb 1
}
struct EFI_TIME
 
EFI_LOADED_IMAGE_PROTOCOL_UUID equ 0A1h,31h,1bh,5bh,62h,95h,0d2h,11h,8Eh,3Fh,0h,0A0h,0C9h,69h,72h,3Bh
struc EFI_LOADED_IMAGE_PROTOCOL {
 .Revision		int32
 .ParentHandle		int64
 .SystemTable		dptr
 .DeviceHandle		int64
 .FilePath		dptr
 .Reserved		int64
 .LoadOptionsSize	int32
 .ImageBase		dptr
 .ImageSize		int64
 .ImageCodeType		int32
 .ImageDataType		int32
 .UnLoad		dptr
}
struct EFI_LOADED_IMAGE_PROTOCOL
 
EFI_BLOCK_IO_PROTOCOL_UUID equ 21h,5bh,4eh,96h,59h,64h,0d2h,11h,8eh,39h,00h,0a0h,0c9h,69h,72h,3bh
struc EFI_BLOCK_IO_PROTOCOL {
 .Revision		int64
 .Media			dptr
 .Reset			dptr
 .ReadBlocks		dptr
 .WriteBlocks		dptr
 .FlushBlocks		dptr
}
struct EFI_BLOCK_IO_PROTOCOL
 
struc EFI_BLOCK_IO_MEDIA {
 .MediaId		int32
 .RemovableMedia	int8
 .MediaPresent		int8
 .LogicalPartition	int8
 .ReadOnly		int8
 .WriteCaching		int8
 .BlockSize		int32
 .IoAlign		int32
 .LastBlock		int64
}
struct EFI_BLOCK_IO_MEDIA
 
EFI_GRAPHICS_OUTPUT_PROTOCOL_UUID equ 0deh, 0a9h, 42h,90h,0dch,023h,38h,04ah,96h,0fbh,7ah,0deh,0d0h,80h,51h,6ah
struc EFI_GRAPHICS_OUTPUT_PROTOCOL {
 .QueryMode		dptr
 .SetMode		dptr
 .Blt			dptr
 .Mode			dptr
}
struct EFI_GRAPHICS_OUTPUT_PROTOCOL
 
struc EFI_GRAPHICS_OUTPUT_PROTOCOL_MODE {
 .MaxMode		int32
 .CurrentMode		int32
 .ModeInfo		dptr
 .SizeOfModeInfo	intn
 .FrameBufferBase	dptr
 .FrameBufferSize	intn
}
struct EFI_GRAPHICS_OUTPUT_PROTOCOL_MODE
 
struc EFI_GRAPHICS_OUTPUT_MODE_INFORMATION {
 .Version		int32
 .HorizontalResolution	int32
 .VerticalResolution	int32
 .PixelFormat		int32
 .RedMask		int32
 .GreenMask		int32
 .BlueMask		int32
 .Reserved		int32
 .PixelsPerScanline	int32
}
struct EFI_GRAPHICS_OUTPUT_MODE_INFORMATION